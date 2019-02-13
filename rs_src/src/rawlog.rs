// Append-only log files, useful for recording security camera
// footage.  Requirements:
// - Don't rely on filesystem.  Make this work on raw storage or bulk files.
// - Binary packet interface.
// - Streaming and appending should be O(1)
// - Out-of-band segmentation.  I.e. don't rely on self-segmenting payloads
// - Allow for mmap implementation.  Assume file fits in vmem.
// 
//
// The write end for this doesn't need a whole lot of processing power
// so it is implemented in Erlang.  It is only indexing and processing
// -- i.e. anything that touched bulk data -- that should be
// efficient.

extern crate libc;

use std::{io, ptr, slice, mem, fs};
use std::os::unix::io::AsRawFd;

const MEM_SIZE : u32 = 3 * 4096;

pub struct Chunk {
    mem: *mut libc::c_void,
    len: usize
}

impl Chunk {
    #[allow(dead_code)]
    pub fn chunk(filename: &str) -> Option<Chunk> {
        match fs::metadata(&filename) {
            Err(e) => {
                eprintln!("WARNING: {}: metadata failed: {:?}", filename, e);
                None
            },
            Ok(metadata) => {
                match fs::OpenOptions::new()
                    .read(true)
                    .open(filename) {
                        Err(e) => {
                            eprintln!("WARNING: {}: open failed: {:?}", filename, e);
                            None
                        },
                        Ok(file) => {
                            unsafe {
                                let ptr = libc::mmap(
                                    ptr::null_mut(),
                                    metadata.len() as libc::size_t,
                                    libc::PROT_READ,
                                    libc::MAP_SHARED,
                                    file.as_raw_fd(),
                                    0 as libc::off_t);
                                if ptr == libc::MAP_FAILED {
                                    panic!("mmap failed: {}", io::Error::last_os_error())
                                } else {
                                    Some( Chunk { mem: ptr, len: metadata.len() as usize } )
                                }
                            }
                        }
                    }
            }
        }
    }
    fn get_bytes(&mut self, offset: usize, bytes: &mut [u8]) {
        let n = bytes.len();
        if offset + n  > self.len { panic!("memory out of range"); }
        unsafe {
            let src = self.mem as *mut u8; 
            // eprintln!("src = {:?}", src);
            ptr::copy_nonoverlapping(
                src.offset(offset as isize),
                bytes.as_mut_ptr(),
                n as usize)
        }
    }
    // Wrap in Option, so it can be used to detect eof
    pub fn get_u32_be(&mut self, offset: usize) -> Option<u32> {
        if offset + 4 > self.len { None }
        else {
            let mut buf = [0,0,0,0];
            self.get_bytes(offset, &mut buf);
            Some(buf[3] as u32
                 + (buf[2] as u32) * 0x100
                 + (buf[1] as u32) * 0x10000
                 + (buf[0] as u32) * 0x1000000)
        }
    }
    /* Scan for next packet.  Note that this can yield false
     * positives.  A counterexample is a binary packet containing log
     * fragments.  The real solution is to build an index.  The format
     * itself is not ambiguous. */
    pub fn scan_offset(&mut self, offset: usize, endx: usize) -> Option<usize> {
        let mut o = offset;
        while o < endx {
            match self.verify(o) {
                Some(_size) => { return Some(o); }
                None => { o += 1; }
            }
        }
        None
    }
    fn verify(&mut self, offset: usize) -> Option<u32> {
        let size  = self.get_u32_be(offset)?;
        if offset + (size as usize) + 8 > self.len { return None; }
        let size2 = self.get_u32_be(offset + 4 + size as usize)?;
        if size != size2 { return None };
        /* This is not enougn.  size == 0 is a common false positive,
         * so at least assert it has the ETF prefix. */
        if size == 0 { return None; }
        let mut buf = [0];
        self.get_bytes(offset + 4, &mut buf);
        if buf[0] != 131 { return None }
        /* At this point we are reasonably sure, but it is probably
         * best to perform another data validation step upstream. */
        Some(size)
    }
    pub fn make_index_u32(&mut self) -> Vec<u32> {
        let mut vec = Vec::new();
        let mut o = 0;
        while o < self.len {
            vec.push(o as u32);
            let size = self.get_u32_be(o).unwrap();
            o += size as usize + 8;
        }
        vec
    }
    // Default is u32, since there seems to be no real reason to have
    // chunks larger than 4GB.  This will allow the code to work on 32
    // bit platforms as well while memory-mapping.
    pub fn save_index_u32(&mut self, filename: &str) -> usize {
        // Build the whole thing in memory.  That is probably ok.
        let vec = self.make_index_u32();
        let slice_u32: &[u32] = &vec;
        let slice_u8: &[u8] = unsafe {
            slice::from_raw_parts(
                slice_u32.as_ptr() as *const u8,
                slice_u32.len() * mem::size_of::<u32>())
        };
        fs::write(filename, slice_u8).expect("ERROR: Unable to write file");
        vec.len()
    }

    // Another useful thing is to perform bisection search.  The index
    // only allows direct message lookup, but to find a particular
    // time stamp, we still need to perform search.  Perform the loop
    // in Rust or in Erlang?  On a cold cache, this is going to be
    // disk-bound anyway.
    
}

impl Drop for Chunk {
    fn drop(&mut self) {
        unsafe {
            assert!(
                libc::munmap(self.mem, 4096 as libc::size_t) == 0,
                "unable to unmap mmap: {}",
                io::Error::last_os_error()
            );
        }
    }
}


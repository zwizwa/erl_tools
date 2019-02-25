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

pub struct RawLog {
    mem: *mut libc::c_void,
    len: usize
}

impl<'a> RawLog {
    #[allow(dead_code)]
    pub fn rawlog(filename: &str) -> Option<RawLog> {
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
                                    Some( RawLog { mem: ptr, len: metadata.len() as usize } )
                                }
                            }
                        }
                    }
            }
        }
    }
    pub fn slice(&self, offset: usize, n: usize) -> Option<&'a [u8]> {
        if offset + n  > self.len { return None; }
        unsafe {
            let src = self.mem as *const u8; 
            Some(slice::from_raw_parts(src.offset(offset as isize), n))
        }
    }
    //// With slice, this is not necessary.
    // fn copy(&self, offset: usize, bytes: &mut [u8]) -> Option<()> {
    //     let n = bytes.len();
    //     if offset + n  > self.len { return None; }
    //     unsafe {
    //         let src = self.mem as *mut u8; 
    //         // eprintln!("src = {:?}", src);
    //         Some(
    //             ptr::copy_nonoverlapping(
    //                 src.offset(offset as isize),
    //                 bytes.as_mut_ptr(),
    //                 n as usize))
    //     }
    // }
    // Wrap in Option, so it can be used to detect eof
    pub fn get_u32_be(&self, offset: usize) -> Option<u32> {
        let slice = self.slice(offset, 4)?;
        Some(slice[3] as u32
             + (slice[2] as u32) * 0x100
             + (slice[1] as u32) * 0x10000
             + (slice[0] as u32) * 0x1000000)
    }
    /* Scan for next packet.  Note that this can yield false
     * positives.  If possible, use an unambiguous representation with
     * an index. */
    pub fn scan_offset_etf(&self, offset: usize, endx: usize) -> Option<usize> {
        let mut o = offset;
        while o < endx {
            match self.verify_etf(o) {
                Some(_size) => { return Some(o); }
                None => { o += 1; }
            }
        }
        None
    }
    fn verify_etf(&self, offset: usize) -> Option<u32> {
        let size  = self.get_u32_be(offset)?;
        if offset + (size as usize) + 8 > self.len { return None; }
        let size2 = self.get_u32_be(offset + 4 + size as usize)?;
        if size != size2 { return None };
        /* The above is not enougn.  size == 0 is a common false
           positive, so we don't support it.  Some additional
           redundancy is needed, so at least assert it has the ETF
           prefix. */
        if size == 0 { return None; }
        let slice = self.slice(offset + 4, 1)?;
        if slice[0] != 131 { return None }
        /* At this point we are reasonably sure it's a packet
           containing an ETF bionary, but it is probably
           best to perform another data validation step upstream. */
        Some(size)
    }
    pub fn make_index_u32(&self) -> Vec<u32> {
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
    pub fn save_index_u32(&self, filename: &str) -> usize {
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

    // Iteratore over slices.
    



    // Extract temperature data from log.  This needs to deal with:
    // - Time base resets:  {9,{start,{{2018,5,6},{14,31,55}}}},
    // - Sensor data:       {515,{temp,zoe,22.375}},
    // - Furnace state:     {515,{furnace,on}},
    // All the rest can be ignored

    // I wonder if it is really necessary to do the filtering in Rust.
    // Probably not unless the whole file needs to be traversed.
    // First problem is finding the time marker.

    // I want to translate this into a multiresolution file.  But that
    // only works with data that is on a fixed sampling grid.  Not an
    // issue though.

    


    // Another useful thing is to perform bisection search.  The index
    // only allows direct message lookup, but to find a particular
    // time stamp, we still need to perform search.  Perform the loop
    // in Rust or in Erlang?  On a cold cache, this is going to be
    // disk-bound anyway.
    
}

impl Drop for RawLog {
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
// https://stackoverflow.com/questions/30218886/how-to-implement-iterator-and-intoiterator-for-a-simple-struct
// https://stackoverflow.com/questions/34733811/what-is-the-difference-between-iter-and-into-iter

// Convert log reference to iterator
impl<'a> IntoIterator for &'a RawLog {
    type Item = &'a [u8];
    type IntoIter = RawLogIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        RawLogIterator {
            rawlog: &self,
            offset: 0,
        }
    }
}

// The iterator itself, which is consumed in an iteration.
pub struct RawLogIterator<'a> {
    rawlog: &'a RawLog,
    offset: usize,
}
impl<'a> Iterator for RawLogIterator<'a> {
    type Item = &'a [u8];
    fn next(&mut self) -> Option<&'a [u8]> {
        match self.rawlog.get_u32_be(self.offset) {
            Some(size) => {
                self.offset += size as usize + 8;
                self.rawlog.slice(self.offset, size as usize)
            },
            None => {
                None
            }
        }
    }
}

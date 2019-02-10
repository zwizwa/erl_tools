// A bisectable, append-only log.  Driver is security camera recording.
//
// Requirements:
// - Don't rely on filesystem.  Make this work on raw storage or bulk files.
// - Binary packet interface.
// - Streaming and appending should be O(1)
// - Out-of-band indexing, i.e. don't rely on self-segmenting payloads
// - Allow for mmap implementation.  Assume file fits in vmem.
// 
// If file doesn't fit, use this data structure as an index
//
// Format is size-prefixed binary payloads with index data at fixed
// intervals, e.g. once per memory page.


// FIXME: This really seems like too much work.  Use a 2-file
// approach.  A raw file with just self-delimiting data that can be
// streamed, e.g. {packet,4} wrapped ETF, and an index structure.

// Create the simplest possible index structure: an array of 64bit
// integers containing file offsets.

// This way, no Rust code is needed at all.

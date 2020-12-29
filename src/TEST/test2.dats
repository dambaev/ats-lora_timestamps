#include "share/atspre_staload.hats"

#define ATS_DYNLOADFLAG 0

staload TS="./../SATS/timestamps.sats"
#define LIBS_targetloc "../libs" (* search path for external libs *)
#include "{$LIBS}/ats-bytestring/HATS/bytestring.hats"
staload UN="prelude/SATS/unsafe.sats"


fn
  test1(): void = {
  val src = $BS.pack "1970-01-01T00:00:00.0Z"
  val- ~Some_vt( @(secs, nsecs) ) = $TS.lora_timestamp2timespec( src)
  //val () = println!( "secs = ", secs, ", nsecs=", nsecs)
  val () = assertloc( secs = $UN.cast{int32} 0)
  val () = assertloc( nsecs = $UN.cast{uint32} 0u)
  val () = free src
}  

fn
  test2(): void = {
  val src = $BS.pack "1970-01-01T00:01:00.0Z"
  val- ~Some_vt( @(secs, nsecs) ) = $TS.lora_timestamp2timespec( src)
  //val () = println!( "secs = ", secs, ", nsecs=", nsecs)
  val () = assertloc( secs = $UN.cast{int32} 60)
  val () = assertloc( nsecs = $UN.cast{uint32} 0u)
  val () = free src
}  

fn
  test3(): void = {
  val src = $BS.pack "1971-01-01T00:00:00.0Z"
  val- ~Some_vt( @(secs, nsecs) ) = $TS.lora_timestamp2timespec( src)
  //val () = println!( "secs = ", secs, ", nsecs=", nsecs)
  val () = assertloc( secs = $UN.cast{int32} 31536000)
  val () = assertloc( nsecs = $UN.cast{uint32} 0u)
  val () = free src
}  

fn
  test4(): void = {
  val src = $BS.pack "2020-01-01T00:00:00.0Z"
  val- ~Some_vt( @(secs, nsecs) ) = $TS.lora_timestamp2timespec( src)
  //val () = println!( "secs = ", secs, ", nsecs=", nsecs)
  val () = assertloc( secs = $UN.cast{int32} 1577836800)
  val () = assertloc( nsecs = $UN.cast{uint32} 0u)
  val () = free src
}  

fn
  test5(): void = {
  val src = $BS.pack "2020-12-18T13:27:59.862974379Z"
  val- ~Some_vt( @(secs, nsecs) ) = $TS.lora_timestamp2timespec( src)
  //val () = println!( "secs = ", secs, ", nsecs=", nsecs)
  val () = assertloc( secs = $UN.cast{int32} 1608298079)
  val () = assertloc( nsecs = $UN.cast{uint32} 862974379)
  val () = free src
}  

implement main0() = {
  val () = test1()
//  val () = test2()
//  val () = test3()
//  val () = test4()
//  val () = test5()
}
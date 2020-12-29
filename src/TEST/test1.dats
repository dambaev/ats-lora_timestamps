#include "share/atspre_staload.hats"

#define ATS_DYNLOADFLAG 0

staload TS="./../SATS/timestamps.sats"
#define LIBS_targetloc "../libs" (* search path for external libs *)
#include "{$LIBS}/ats-bytestring/HATS/bytestring.hats"
staload UN="prelude/SATS/unsafe.sats"


fn
  test1(): void = {
  val yday = $TS.get_yday_from_date( $UN.cast{uint32} 1970, $UN.cast{uint32}1, $UN.cast{uint32} 1)
  val () = println!( "yday(1970,1,1) = ", yday)
  val () = assertloc( yday = $UN.cast{uint32}1)
}  

fn
  test2(): void = {
  val yday = $TS.get_yday_from_date( $UN.cast{uint32} 1970, $UN.cast{uint32}1, $UN.cast{uint32} 2)
  val () = println!( "yday(1970,1,2) = ", yday)
  val () = assertloc( yday = $UN.cast{uint32}2)
}  

fn
  test3(): void = {
  val yday = $TS.get_yday_from_date( $UN.cast{uint32} 1970, $UN.cast{uint32}2, $UN.cast{uint32} 1)
  val () = println!( "yday(1970,2,1) = ", yday)
  val () = assertloc( yday = $UN.cast{uint32}32)
}  

fn
  test4(): void = {
  val yday = $TS.get_yday_from_date( $UN.cast{uint32} 1971, $UN.cast{uint32}2, $UN.cast{uint32} 1)
  val () = println!( "yday(1971,2,1) = ", yday)
  val () = assertloc( yday = $UN.cast{uint32}32)
}  

fn
  test5(): void = {
  val yday = $TS.get_yday_from_date( $UN.cast{uint32} 1971, $UN.cast{uint32}12, $UN.cast{uint32} 1)
  val () = println!( "yday(1971,12,1) = ", yday)
  val () = assertloc( yday = $UN.cast{uint32}335)
}  

fn
  test6(): void = {
  val yday = $TS.get_yday_from_date( $UN.cast{uint32} 2020, $UN.cast{uint32}12, $UN.cast{uint32} 1)
  val () = println!( "yday(2020,12,1) = ", yday)
  val () = assertloc( yday = $UN.cast{uint32}336)
}  

implement main0() = {
  val () = test1()
  val () = test2()
  val () = test3()
  val () = test4()
  val () = test5()
  val () = test6()
}
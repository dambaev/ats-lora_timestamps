
#define ATS_PACKNAME "timestamps"
#define ATS_EXTERN_PREFIX "timestamps"
#include "share/atspre_staload.hats" // include template definitions

#define LIBS_targetloc "../libs" (* search path for external libs *)
#include "{$LIBS}/ats-bytestring/HATS/bytestring.hats"

(* this function should convert string timestamp into timespec struct
 * the example value of lora timestamp is "2020-12-18T13:27:59.862974379Z"
 * https://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap04.html#tag_04_14
 tm_sec + tm_min*60 + tm_hour*3600 + tm_yday*86400 +
    (tm_year-70)*31536000 + ((tm_year-69)/4)*86400 -
    ((tm_year-1)/100)*86400 + ((tm_year+299)/400)*86400
 *)
fn
  lora_timestamp2timespec
  {offset,cap,ucap:nat}{len:pos}{dynamic:bool}{p:agz}
  ( i: !$BS.Bytestring_vtype( len, offset, cap, ucap, 0, dynamic, p)
  ):<!wrt> Option_vt( @(int32, uint32) )


(* calculates the sequence number of day of the given date, starting from the Jan 01, where Jan 01 is the day 1 and the Dec 31 is either 365 or 366 *)
fn
  get_yday_from_date
  ( year: uint32
  , month: uint32
  , day: uint32
  ):<> uint32
namespace Grok.Lib

open Newtonsoft.Json

module Library =
  let getJsonNetJson value =
      sprintf "I used to be %s but now I'm %s thanks to JSON.NET!" value (JsonConvert.SerializeObject(value))

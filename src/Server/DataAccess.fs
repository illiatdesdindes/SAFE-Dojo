module DataAccess

open FSharp.Data
open Shared

module GeoLocationAPI =
    open FSharp.Data.UnitSystems.SI.UnitNames

    type PostcodesIO = JsonProvider<"http://api.postcodes.io/postcodes/EC2A4NE">

    let buildLocationFromResult (result: PostcodesIO.Root): Location =
        { LatLong =
              { Latitude = float result.Result.Latitude
                Longitude = float result.Result.Longitude }
          Postcode = result.Result.Postcode
          Town = result.Result.AdminDistrict
          Region = result.Result.Nuts
          AdminWard = result.Result.AdminWard }

    let getRandomLocation =
        async {
            let! postcodeData = PostcodesIO.AsyncLoad "http://api.postcodes.io/random/postcodes"

            return buildLocationFromResult postcodeData
        }

    let getLocation postcode =
        async {
            let! postcode =
                postcode
                |> sprintf "http://api.postcodes.io/postcodes/%s"
                |> PostcodesIO.AsyncLoad

            return buildLocationFromResult postcode
        }

    let getDistanceBetweenPositions pos1 pos2 =
        let lat1, lng1 = pos1.Latitude, pos1.Longitude
        let lat2, lng2 = pos2.Latitude, pos2.Longitude
        let inline degreesToRadians degrees = System.Math.PI * float degrees / 180.0
        let r = 6371000.0
        let phi1 = degreesToRadians lat1
        let phi2 = degreesToRadians lat2
        let deltaPhi = degreesToRadians (lat2 - lat1)
        let deltaLambda = degreesToRadians (lng2 - lng1)

        let a =
            sin (deltaPhi / 2.0)
            * sin (deltaPhi / 2.0)
            + cos phi1
              * cos phi2
              * sin (deltaLambda / 2.0)
              * sin (deltaLambda / 2.0)

        let c = 2.0 * atan2 (sqrt a) (sqrt (1.0 - a))
        r * c * 1.<meter>

module CrimeAPI =
    type PoliceUkCrime = JsonProvider<"https://data.police.uk/api/crimes-street/all-crime?lat=51.5074&lng=0.1278">

    let getCrimesNearPosition location =
        (location.Latitude, location.Longitude)
        ||> sprintf "https://data.police.uk/api/crimes-street/all-crime?lat=%f&lng=%f"
        |> PoliceUkCrime.AsyncLoad

module WeatherAPI =
    type MetaWeatherSearch = JsonProvider<"https://www.metaweather.com/api/location/search/?lattlong=51.5074,0.1278">
    type MetaWeatherLocation = JsonProvider<"https://www.metaweather.com/api/location/1393672">

    let getWeatherForPosition location =
        async {
            let! locations =
                (location.Latitude, location.Longitude)
                ||> sprintf "https://www.metaweather.com/api/location/search/?lattlong=%f,%f"
                |> MetaWeatherSearch.AsyncLoad

            let bestLocationId =
                locations
                |> Array.sortBy (fun t -> t.Distance)
                |> Array.map (fun o -> o.Woeid)
                |> Array.head

            return! bestLocationId
                    |> sprintf "https://www.metaweather.com/api/location/%d"
                    |> MetaWeatherLocation.AsyncLoad
        }

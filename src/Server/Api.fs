module Api

open DataAccess
open FSharp.Data.UnitSystems.SI.UnitNames
open Shared

let private london =
    { Latitude = 51.5074
      Longitude = 0.1278 }

let createLocationResponse location =
    let distanceToLondon =
        GeoLocationAPI.getDistanceBetweenPositions location.LatLong london

    { Postcode = location.Postcode
      Location = location
      DistanceToLondon = (distanceToLondon / 1000.<meter>) }

let getDistanceFromLondon postcode =
    async {
        if not (Validation.isValidPostcode postcode)
        then failwith "Invalid postcode"

        let! location = GeoLocationAPI.getLocation postcode

        let distanceToLondon =
            GeoLocationAPI.getDistanceBetweenPositions location.LatLong london

        return createLocationResponse location
    }

let getRandomLocation () =
    async {
        let! location = GeoLocationAPI.getRandomLocation

        return createLocationResponse location
    }

let getCrimeReport postcode =
    async {
        if not (Validation.isValidPostcode postcode)
        then failwith "Invalid postcode"

        let! location = GeoLocationAPI.getLocation postcode
        let! reports = CrimeAPI.getCrimesNearPosition location.LatLong

        let crimes =
            reports
            |> Array.countBy (fun r -> r.Category)
            |> Array.sortByDescending snd
            |> Array.map (fun (k, c) -> { Crime = k; Incidents = c })

        return crimes
    }

let private asWeatherResponse (weather: DataAccess.WeatherAPI.MetaWeatherLocation.Root) =
    { WeatherType =
          weather.ConsolidatedWeather
          |> Array.countBy (fun w -> w.WeatherStateName)
          |> Array.maxBy snd
          |> fst
          |> WeatherType.Parse
      AverageTemperature =
          weather.ConsolidatedWeather
          |> Array.averageBy (fun r -> float r.TheTemp) }

let getWeather postcode =
    async {
        let! location = GeoLocationAPI.getLocation (postcode)
        let! weatherResponse = WeatherAPI.getWeatherForPosition location.LatLong

        return asWeatherResponse weatherResponse
    }

let dojoApi =
    { GetDistance = getDistanceFromLondon
      GetRandomLocation = getRandomLocation
      GetCrimes = getCrimeReport
      GetWeather = getWeather

    (* Task 4.2 WEATHER: Hook up the weather endpoint to the getWeather function. *)
    }

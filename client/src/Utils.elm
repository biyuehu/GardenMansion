module Utils exposing (errorToString, formatTime, getUserName, monthToInt)

import Dict
import Http
import Time
import Types exposing (Model)

errorToString : Http.Error -> String
errorToString err =
  case err of
    Http.BadUrl url -> "无效的URL: " ++ url
    Http.Timeout -> "请求超时，请检查网络连接"
    Http.NetworkError -> "网络错误，请检查网络连接"
    Http.BadStatus code ->
      case code of
        400 -> "请求参数有误，请检查输入"
        401 -> "登录已过期、账户或密码错误，请重新登录"
        403 -> "权限不足，无法执行此操作"
        404 -> "请求的资源不存在"
        405 -> "方法不允许"
        429 -> "请求过于频繁，请稍后再试"
        500 -> "服务器内部错误，请稍后重试"
        502 -> "网关错误，请稍后重试"
        503 -> "服务不可用，请稍后重试"
        _ -> "服务器返回错误状态码: " ++ String.fromInt code
    Http.BadBody msg -> "数据解析失败: " ++ msg

monthToInt : Time.Month -> Int
monthToInt month =
  case month of
    Time.Jan -> 1
    Time.Feb -> 2
    Time.Mar -> 3
    Time.Apr -> 4
    Time.May -> 5
    Time.Jun -> 6
    Time.Jul -> 7
    Time.Aug -> 8
    Time.Sep -> 9
    Time.Oct -> 10
    Time.Nov -> 11
    Time.Dec -> 12

formatTime : Float -> String
formatTime timestamp =
  let
    posix = Time.millisToPosix (round timestamp)
    year = String.fromInt (Time.toYear Time.utc posix)
    month = String.fromInt (monthToInt (Time.toMonth Time.utc posix)) |> String.padLeft 2 '0'
    day = String.fromInt (Time.toDay Time.utc posix) |> String.padLeft 2 '0'
    hour = String.fromInt (Time.toHour Time.utc posix) |> String.padLeft 2 '0'
    minute = String.fromInt (Time.toMinute Time.utc posix) |> String.padLeft 2 '0'
    second = String.fromInt (Time.toSecond Time.utc posix) |> String.padLeft 2 '0'
  in
  year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second

getUserName : Model -> Int -> String
getUserName model userId =
  case Dict.get userId model.userDict of
    Just name -> name
    Nothing -> "用户 #" ++ String.fromInt userId

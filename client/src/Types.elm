module Types exposing (Model, Page(..), Msg(..), initialModel)

import Dict exposing (Dict)
import Http
import Models exposing (ResMessageSingle, ResExpenseSingle, ResUserSingle, ResInfoApi, ResMetaApi)
import Time

type alias Model =
  { currentPage : Page
  , token : Maybe String
  , loginUsername : String
  , loginPassword : String
  , loginError : Maybe String
  , messages : List ResMessageSingle
  , expenses : List ResExpenseSingle
  , users : List ResUserSingle
  , info : Maybe ResInfoApi
  , meta : Maybe ResMetaApi
  , newMessage : String
  , newMessageReplyId : Maybe Int
  , newExpenseAmount : String
  , newExpenseComment : String
  , newUserName : String
  , newUserNickname : String
  , newUserPassword : String
  , renameInput : String
  , pwOldInput : String
  , pwNewInput : String
  , metaUrlInput : String
  , metaNameInput : String
  , metaTitleInput : String
  , metaNoticeInput : String
  , metaStartTimeInput : String
  , errorMsg : Maybe String
  , statusMsg : Maybe String
  , userDict : Dict Int String
  , currentTime : Time.Posix
  }

type Page
  = LoginPage
  | MessagesPage
  | ExpensesPage
  | UsersPage
  | InfoPage
  | MetaPage

type Msg
  = ChangePage Page
  | UpdateLoginUsername String
  | UpdateLoginPassword String
  | SubmitLogin
  | GotLoginResult (Result Http.Error Models.ResLoginApi)
  | Logout
  | GotMeta (Result Http.Error ResMetaApi)
  | UpdateMetaUrl String
  | UpdateMetaName String
  | UpdateMetaTitle String
  | UpdateMetaNotice String
  | SubmitMeta
  | GotMetaUpdateResult (Result Http.Error ())
  | GotInfo (Result Http.Error ResInfoApi)
  | UpdateRenameInput String
  | SubmitRename
  | GotRenameResult (Result Http.Error ())
  | UpdatePwOldInput String
  | UpdatePwNewInput String
  | SubmitPasswordChange
  | GotPasswordResult (Result Http.Error ())
  | GotMessages (Result Http.Error Models.ResMessageApi)
  | UpdateNewMessage String
  | SetReplyTo (Maybe Int)
  | SubmitMessage
  | GotMessagePostResult (Result Http.Error ())
  | DeleteMessage Int
  | GotMessageDeleteResult (Result Http.Error ())
  | GotExpenses (Result Http.Error Models.ResExpenseApi)
  | UpdateExpenseAmount String
  | UpdateExpenseComment String
  | SubmitExpense
  | GotExpensePostResult (Result Http.Error ())
  | DeleteExpense Int
  | GotExpenseDeleteResult (Result Http.Error ())
  | GotUsers (Result Http.Error Models.ResUserApi)
  | UpdateNewUserName String
  | UpdateNewUserNickname String
  | UpdateNewUserPassword String
  | SubmitUser
  | GotUserPostResult (Result Http.Error ())
  | DeleteUser Int Bool
  | GotUserDeleteResult (Result Http.Error ())
  | DismissError
  | GotCurrentTime Time.Posix
  | UpdateMetaStartTime String

initialModel : Model
initialModel =
  { currentPage = LoginPage
  , token = Nothing
  , loginUsername = ""
  , loginPassword = ""
  , loginError = Nothing
  , messages = []
  , expenses = []
  , users = []
  , info = Nothing
  , meta = Nothing
  , newMessage = ""
  , newMessageReplyId = Nothing
  , newExpenseAmount = ""
  , newExpenseComment = ""
  , newUserName = ""
  , newUserNickname = ""
  , newUserPassword = ""
  , renameInput = ""
  , pwOldInput = ""
  , pwNewInput = ""
  , metaUrlInput = ""
  , metaNameInput = ""
  , metaTitleInput = ""
  , metaNoticeInput = ""
  , metaStartTimeInput = ""
  , errorMsg = Nothing
  , statusMsg = Nothing
  , userDict = Dict.empty
  , currentTime = Time.millisToPosix 0
  }

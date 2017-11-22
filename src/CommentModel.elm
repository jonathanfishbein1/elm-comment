module CommentModel
    exposing
        ( CommentId
        , CommentIdModel(CommentIdModel)
        , CommentModel
        , CommentMsg(..)
        , UserCommentModel
        , UserIdModel(UserIdModel)
        , commentInitState
        , commentModelInit
        , commentZipperInit
        , config
        , destructureCommentIdModel
        )

import AutoExpand
import MultiwayTree exposing (Tree)
import MultiwayTreeZipper exposing (Zipper)


commentOrLogin :
    Bool
    -> CommentId
    -> { state : AutoExpand.State, textValue : String }
    -> CommentMsg
commentOrLogin isSignedIn parentCommentId state =
    if isSignedIn == True then
        TextInput parentCommentId state
    else
        CommentLogin


commentInitState =
    AutoExpand.initState <| config False ""


config : Bool -> CommentId -> AutoExpand.Config CommentMsg
config isSignedIn parentCommentId =
    AutoExpand.config
        { onInput = commentOrLogin isSignedIn parentCommentId
        , padding = 10
        , lineHeight = 20
        , minRows = 1
        , maxRows = 4
        }
        |> AutoExpand.withStyles [ ( "font-family", "sans-serif" ) ]
        |> AutoExpand.withPlaceholder "Write a Comment"


destructureCommentIdModel : CommentIdModel -> CommentId
destructureCommentIdModel (CommentIdModel commentId) =
    commentId


type CommentMsg
    = ClickReplyButton CommentId
    | TextInput CommentId { textValue : String, state : AutoExpand.State }
    | GenerateCommentId UserCommentModel CommentId
    | ClearValue CommentId
    | CommentRouting String
    | CommentLogin


type alias UserCommentModel =
    { userId : UserIdModel
    , userFullName : String
    , picture : String
    }


type alias CommentModel =
    { commentId : CommentIdModel
    , user : UserCommentModel
    , message : String
    , showReplyInput : Bool
    , protoMessage : String
    , autoexpand : AutoExpand.State
    }


type CommentIdModel
    = CommentIdModel CommentId


type alias CommentId =
    String


type alias UserId =
    String


type UserIdModel
    = UserIdModel String


destructureUserId : UserIdModel -> UserId
destructureUserId (UserIdModel userId) =
    userId


commentModelInit : CommentModel
commentModelInit =
    CommentModel (CommentIdModel "") userCommentModelInit "" False "" (AutoExpand.initState <| config False "")


userCommentModelInit : UserCommentModel
userCommentModelInit =
    UserCommentModel (UserIdModel "") "" ""


commentTreeInit : Tree CommentModel
commentTreeInit =
    MultiwayTree.Tree commentModelInit []


commentZipperInit : Maybe (Zipper CommentModel)
commentZipperInit =
    Just ( commentTreeInit, [] )

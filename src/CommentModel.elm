module CommentModel
    exposing
        ( CommentId
        , CommentIdModel(CommentIdModel)
        , CommentModel
        , CommentMsg(..)
        , UserCommentModel
        , UserIdModel(UserIdModel)
        , commentModelInit
        , commentZipperInit
        , config
        , destructureCommentId
        )

import AutoExpand
import MultiwayTree exposing (Tree)
import MultiwayTreeZipper exposing (Zipper)


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


config : Bool -> CommentId -> AutoExpand.Config CommentMsg
config isSignedIn parentCommentId =
    AutoExpand.config
        { onInput = TextInput parentCommentId
        , padding = 10
        , lineHeight = 20
        , minRows = 1
        , maxRows = 4
        }
        |> AutoExpand.withStyles [ ( "font-family", "sans-serif" ) ]
        |> AutoExpand.withPlaceholder "Write a Comment"


type CommentMsg
    = ClickReplyButton CommentId
    | TextInput CommentId { textValue : String, state : AutoExpand.State }
    | GenerateCommentId UserCommentModel CommentId
    | ClearValue CommentId


type alias UserId =
    String


type UserIdModel
    = UserIdModel UserId


type alias UserCommentModel =
    { userId : UserIdModel
    , userFullName : String
    , picture : String
    }


type alias CommentId =
    String


type CommentIdModel
    = CommentIdModel CommentId


type alias CommentModel =
    { commentId : CommentIdModel
    , user : UserCommentModel
    , message : String
    , showReplyInput : Bool
    , protoMessage : String
    , autoexpand : AutoExpand.State
    }


destructureCommentId : CommentIdModel -> CommentId
destructureCommentId (CommentIdModel commentId) =
    commentId

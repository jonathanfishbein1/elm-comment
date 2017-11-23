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
        , commentTreeInit
        , commentZipperInit
        , config
        , destructureCommentIdModel
        )

{-| This library is a comments library similar to that found on social media sites.


# Types

@docs CommentId, CommentIdModel, CommentModel, CommentMsg, UserCommentModel, UserIdModel, config


# Inital values

@docs commentInitState, commentModelInit, commentTreeInit, commentZipperInit


# Helper functions

@docs destructureCommentIdModel

-}

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


{-| Initial comment state
-}
commentInitState : AutoExpand.State
commentInitState =
    AutoExpand.initState <| config False ""


{-| Comment config: accepts a isSignedIn boolean flag and a parentCommentId
-}
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


{-| Comment Messages
-}
type CommentMsg
    = ClickReplyButton CommentId
    | TextInput CommentId { textValue : String, state : AutoExpand.State }
    | GenerateCommentId UserCommentModel CommentId
    | ClearValue CommentId
    | CommentRouting String
    | CommentLogin


type alias UserId =
    String


{-| Union Wrapped UserId
-}
type UserIdModel
    = UserIdModel String


{-| UserModel for comments
-}
type alias UserCommentModel =
    { userId : UserIdModel
    , userFullName : String
    , picture : String
    }


{-| CommentId
-}
type alias CommentId =
    String


{-| Union Wrapped CommentId
-}
type CommentIdModel
    = CommentIdModel CommentId


{-| CommentModel
-}
type alias CommentModel =
    { commentId : CommentIdModel
    , user : UserCommentModel
    , message : String
    , showReplyInput : Bool
    , protoMessage : String
    , autoexpand : AutoExpand.State
    }


destructureUserId : UserIdModel -> UserId
destructureUserId (UserIdModel userId) =
    userId


{-| destructure a CommentIdModel to CommentId
-}
destructureCommentIdModel : CommentIdModel -> CommentId
destructureCommentIdModel (CommentIdModel commentId) =
    commentId


{-| CommentModel initial value
-}
commentModelInit : CommentModel
commentModelInit =
    CommentModel (CommentIdModel "") userCommentModelInit "" False "" (AutoExpand.initState <| config False "")


userCommentModelInit : UserCommentModel
userCommentModelInit =
    UserCommentModel (UserIdModel "") "" ""


{-| CommentTree initial value
-}
commentTreeInit : Tree CommentModel
commentTreeInit =
    MultiwayTree.Tree commentModelInit []


{-| Zipper initial value
-}
commentZipperInit : Maybe (Zipper CommentModel)
commentZipperInit =
    Just ( commentTreeInit, [] )

module CommentView
    exposing
        ( commentMainView
        )

{-| This library is a comments library similar to that found on social media sites.

@docs commentMainView

-}

import CommentModel
    exposing
        ( CommentId
        , CommentIdModel(CommentIdModel)
        , CommentModel
        , CommentMsg(ClickReplyButton, CommentRouting, GenerateCommentId)
        , UserCommentModel
        , UserIdModel(UserIdModel)
        )
import CommentStyles exposing (Class(..))
import Element
    exposing
        ( Element
        , button
        , column
        , el
        , html
        , link
        , paragraph
        , row
        , section
        , text
        )
import Internal.CommentView
    exposing
        ( addCommentView
        , individualCommentView
        )
import MultiwayTree
    exposing
        ( Forest
        , Tree
        , children
        , datum
        )
import MultiwayTreeZipper exposing (Zipper)


{-| Main comment view function. Use mapAll from style-elements

    Element.mapAll MyCommentMessageWrapper Comment identity (commentMainView isSignedIn userCommentModel myAppComments)

-}
commentMainView : Bool -> UserCommentModel -> Maybe (Zipper CommentModel) -> Element Class variation CommentMsg
commentMainView isSignedIn currentUser zipper =
    section None [] <|
        column None
            []
            (case zipper of
                Just zipper ->
                    let
                        treeCommentModel =
                            Tuple.first zipper

                        commentModel =
                            datum treeCommentModel
                    in
                    [ addCommentView isSignedIn currentUser (Just zipper) commentModel.commentId
                    , column None [] (List.map (individualCommentView isSignedIn currentUser (Just zipper)) (children treeCommentModel))
                    ]

                Nothing ->
                    []
            )

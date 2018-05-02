module CommentView
    exposing
        ( commentMainView
        )

{-| This library is a comments library similar to that found on social media sites.

@docs commentMainView

-}

import CommentModel
    exposing
        ( CommentModel
        , CommentMsg
        , UserCommentModel
        )
import Element
    exposing
        ( Element
        , column
        )
import Internal.CommentView
    exposing
        ( addCommentView
        , individualCommentView
        )
import MultiwayTree
    exposing
        ( children
        , datum
        )
import MultiwayTreeZipper exposing (Zipper)


{-| Main comment view function. Use mapAll from style-elements

    Element.mapAll MyCommentMessageWrapper Comment identity (commentMainView isSignedIn userCommentModel myAppComments)

-}
commentMainView : Bool -> UserCommentModel -> Maybe (Zipper CommentModel) -> Element CommentMsg
commentMainView isSignedIn currentUser zipperMaybe =
    column
        []
        (case zipperMaybe of
            Just zipper ->
                let
                    treeCommentModel =
                        Tuple.first zipper

                    commentModel =
                        datum treeCommentModel
                in
                [ addCommentView isSignedIn currentUser (Just zipper) commentModel.commentId
                , column [] (List.map (individualCommentView isSignedIn currentUser (Just zipper)) (children treeCommentModel))
                ]

            Nothing ->
                []
        )

module CommentView
    exposing
        ( commentMainView
        )

{-| This library is a comments library similar to that found on social media sites.

@docs commentMainView

-}

import AutoExpand
import CommentModel
    exposing
        ( CommentId
        , CommentIdModel(CommentIdModel)
        , CommentModel
        , CommentMsg(ClickReplyButton, CommentRouting, GenerateCommentId)
        , UserCommentModel
        , UserIdModel(UserIdModel)
        , config
        , destructureCommentIdModel
        )
import CommentStyles exposing (Class(..))
import CommentUpdate exposing (getParentComment)
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
import Element.Attributes
    exposing
        ( attribute
        , classList
        , moveRight
        , px
        , width
        )
import Element.Events exposing (onClick)
import MultiwayTree
    exposing
        ( Forest
        , Tree
        , children
        , datum
        )
import MultiwayTreeZipper exposing (Zipper)
import OnClickPreventDefault exposing (onClickPreventDefault)
import String exposing (isEmpty)


{-| Main comment view function
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


addCommentView : Bool -> UserCommentModel -> Maybe (Zipper CommentModel) -> CommentIdModel -> Element Class variation CommentMsg
addCommentView isSignedIn commenter zipper (CommentIdModel parentCommentId) =
    let
        parentCommentModel =
            getParentComment zipper parentCommentId
    in
    column None
        []
        (case parentCommentModel of
            Just parentCommentModel ->
                let
                    isPostCommentButtonDisabled =
                        isEmpty parentCommentModel.protoMessage || isSignedIn == False

                    commenterUserModel =
                        UserCommentModel commenter.userId commenter.userFullName commenter.picture
                in
                [ commentProfileHtml commenterUserModel
                , row None
                    []
                    [ html <| AutoExpand.view (config isSignedIn parentCommentId) parentCommentModel.autoexpand parentCommentModel.protoMessage
                    , button None
                        [ onClick <| GenerateCommentId commenterUserModel parentCommentId
                        , if isPostCommentButtonDisabled then
                            attribute "disabled" ""
                          else
                            classList []
                        ]
                        (text "post")
                    ]
                ]

            Nothing ->
                []
        )


forestView : Bool -> UserCommentModel -> Maybe (Zipper CommentModel) -> Forest CommentModel -> Element Class variation CommentMsg
forestView isSignedIn currentUser zipper forest =
    column None [ moveRight 20 ] (List.map (individualCommentView isSignedIn currentUser zipper) forest)


commentProfileHtml : UserCommentModel -> Element Class variation CommentMsg
commentProfileHtml commenter =
    let
        (UserIdModel commentUserId) =
            commenter.userId
    in
    link ("profile/" ++ commentUserId) <|
        el Paragraph
            [ onClickPreventDefault <| CommentRouting commentUserId ]
            (text commenter.userFullName)


individualCommentView : Bool -> UserCommentModel -> Maybe (Zipper CommentModel) -> Tree CommentModel -> Element Class variation CommentMsg
individualCommentView isSignedIn currentUser zipper treeCommentModel =
    let
        commentModel =
            datum treeCommentModel
    in
    column None
        []
        [ commentProfileHtml commentModel.user
        , paragraph Paragraph [] [ text commentModel.message ]
        , button None
            [ onClick <| ClickReplyButton <| destructureCommentIdModel commentModel.commentId
            , width <| px 50
            ]
            (text "reply")
        , if commentModel.showReplyInput == True then
            column None [ moveRight 20 ] [ addCommentView isSignedIn currentUser zipper commentModel.commentId ]
          else
            column None [] []
        , column None [] [ forestView isSignedIn currentUser zipper (children treeCommentModel) ]
        ]

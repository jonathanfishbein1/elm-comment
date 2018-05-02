module Internal.CommentView
    exposing
        ( addCommentView
        , commentProfileHtml
        , forestView
        , individualCommentView
        )

import AutoExpand
import CommentModel
    exposing
        ( CommentIdModel(CommentIdModel)
        , CommentModel
        , CommentMsg(ClickReplyButton, CommentRouting, GenerateCommentId)
        , UserCommentModel
        , UserIdModel(UserIdModel)
        , config
        )
import CommentUpdate exposing (getParentComment)
import Element
    exposing
        ( Element
        , column
        , html
        , link
        , paragraph
        , row
        , text
        )
import Framework.Button
import Framework.Modifiers
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


addCommentView : Bool -> UserCommentModel -> Maybe (Zipper CommentModel) -> CommentIdModel -> Element CommentMsg
addCommentView isSignedIn commenter zipper parentCommentIdModel =
    let
        parentCommentModel =
            getParentComment zipper parentCommentIdModel

        (CommentIdModel parentCommentId) =
            parentCommentIdModel
    in
    column
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
                , row
                    []
                    [ html <| AutoExpand.view (config isSignedIn parentCommentId) parentCommentModel.autoexpand parentCommentModel.protoMessage
                    , Framework.Button.button
                        (if isPostCommentButtonDisabled then
                            [ Framework.Modifiers.Disabled ]
                         else
                            []
                        )
                        (Just <| GenerateCommentId commenterUserModel parentCommentIdModel)
                        "post"
                    ]
                ]

            Nothing ->
                []
        )


forestView : Bool -> UserCommentModel -> Maybe (Zipper CommentModel) -> Forest CommentModel -> Element CommentMsg
forestView isSignedIn currentUser zipper forest =
    column [ Element.moveRight 20 ] (List.map (individualCommentView isSignedIn currentUser zipper) forest)


commentProfileHtml : UserCommentModel -> Element CommentMsg
commentProfileHtml commenter =
    let
        (UserIdModel commentUserId) =
            commenter.userId
    in
    link [ Element.htmlAttribute <| onClickPreventDefault <| CommentRouting commentUserId ]
        { url = "profile/" ++ commentUserId
        , label = text commenter.userFullName
        }


individualCommentView : Bool -> UserCommentModel -> Maybe (Zipper CommentModel) -> Tree CommentModel -> Element CommentMsg
individualCommentView isSignedIn currentUser zipper treeCommentModel =
    let
        commentModel =
            datum treeCommentModel
    in
    column
        []
        [ commentProfileHtml commentModel.user
        , paragraph [] [ text commentModel.message ]
        , Framework.Button.button
            []
            (Just <| ClickReplyButton commentModel.commentId)
            "reply"
        , if commentModel.showReplyInput == True then
            column [ Element.moveRight 20 ] [ addCommentView isSignedIn currentUser zipper commentModel.commentId ]
          else
            column [] []
        , column [] [ forestView isSignedIn currentUser zipper (children treeCommentModel) ]
        ]

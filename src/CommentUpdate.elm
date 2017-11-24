module CommentUpdate
    exposing
        ( commentUpdate
        , getParentComment
        )

{-| This library is a comments library similar to that found on social media sites.

@docs commentUpdate, getParentComment

-}

import AutoExpand
import CommentModel
    exposing
        ( CommentId
        , CommentIdModel(CommentIdModel)
        , CommentModel
        , CommentMsg(..)
        , config
        , destructureCommentIdModel
        )
import Maybe exposing (andThen)
import MultiwayTree
import MultiwayTreeZipper
    exposing
        ( Zipper
        , datum
        , goTo
        , goToRoot
        , insertChild
        , updateDatum
        )
import Random.Pcg exposing (Seed, step)
import Uuid


{-| Update function. A seed is necessary

    ( ( newZipper, newSeed ), newCommentCmd ) =
        commentUpdate isSignedIn seed commentMsg myAppComments

-}
commentUpdate : Bool -> Seed -> CommentMsg -> Maybe (Zipper CommentModel) -> ( ( Maybe (Zipper CommentModel), Seed ), Cmd CommentMsg )
commentUpdate isSignedIn seed commentMsg zipper =
    case commentMsg of
        ClearValue parentCommentId ->
            let
                newZipper =
                    zipper
                        |> andThen (goTo (\elem -> destructureCommentIdModel elem.commentId == parentCommentId))
                        |> andThen (updateDatum (\old -> { old | protoMessage = "" }))
                        |> andThen goToRoot
            in
            newZipper
                |> commentUpdate isSignedIn seed (ClickReplyButton parentCommentId)

        ClickReplyButton commentId ->
            let
                newZipper =
                    zipper
                        |> andThen (goTo (\elem -> destructureCommentIdModel elem.commentId == commentId))
                        |> andThen (updateDatum (\old -> { old | showReplyInput = not old.showReplyInput }))
                        |> andThen goToRoot
            in
            ( newZipper, seed ) ! []

        TextInput parentCommentId { state, textValue } ->
            let
                newZipper =
                    zipper
                        |> andThen (goTo (\elem -> destructureCommentIdModel elem.commentId == parentCommentId))
                        |> andThen
                            (updateDatum
                                (\old ->
                                    { old
                                        | autoexpand = state
                                        , protoMessage = textValue
                                    }
                                )
                            )
                        |> andThen goToRoot
            in
            ( newZipper, seed ) ! []

        GenerateCommentId commenterUserModel parentCommentId ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator seed

                parentProtoMessage =
                    getParentComment zipper parentCommentId
            in
            case parentProtoMessage of
                Just protoMessage ->
                    let
                        newZipper =
                            zipper
                                |> andThen (goTo (\elem -> destructureCommentIdModel elem.commentId == parentCommentId))
                                |> andThen (insertChild (MultiwayTree.Tree (CommentModel (CommentIdModel <| Uuid.toString newUuid) commenterUserModel protoMessage.protoMessage False "" (AutoExpand.initState <| config isSignedIn parentCommentId)) []))
                                |> andThen goToRoot
                    in
                    newZipper
                        |> commentUpdate isSignedIn newSeed (ClearValue parentCommentId)

                Nothing ->
                    ( zipper, seed ) ! []

        CommentRouting userId ->
            ( zipper, seed ) ! []

        CommentLogin ->
            ( zipper, seed ) ! []


{-| Function to get the parent comment
-}
getParentComment : Maybe (Zipper CommentModel) -> CommentId -> Maybe CommentModel
getParentComment zipper parentCommentId =
    let
        parentZipper =
            zipper
                |> andThen (goTo (\elem -> destructureCommentIdModel elem.commentId == parentCommentId))
    in
    Maybe.map datum parentZipper

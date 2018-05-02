module CommentUpdate
    exposing
        ( commentUpdate
        , getParentComment
        )

{-| This library is a comments library similar to that found on social media sites.

@docs commentUpdate, getParentComment

-}

import CommentModel
    exposing
        ( CommentIdModel(CommentIdModel)
        , CommentModel
        , CommentMsg(..)
        , commentInitState
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
        ClearValue parentCommentIdModel ->
            let
                newZipper =
                    zipper
                        |> andThen (goTo (\elem -> elem.commentId == parentCommentIdModel))
                        |> andThen (updateDatum (\old -> { old | protoMessage = "" }))
                        |> andThen goToRoot
            in
            newZipper
                |> commentUpdate isSignedIn seed (ClickReplyButton parentCommentIdModel)

        ClickReplyButton commentIdModel ->
            let
                newZipper =
                    zipper
                        |> andThen (goTo (\elem -> elem.commentId == commentIdModel))
                        |> andThen (updateDatum (\old -> { old | showReplyInput = not old.showReplyInput }))
                        |> andThen goToRoot
            in
            ( newZipper, seed ) ! []

        TextInput parentCommentIdModel { state, textValue } ->
            let
                newZipper =
                    zipper
                        |> andThen (goTo (\elem -> elem.commentId == parentCommentIdModel))
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

        GenerateCommentId commenterUserModel parentCommentIdModel ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator seed

                parentProtoMessage =
                    getParentComment zipper parentCommentIdModel

                (CommentIdModel _) =
                    parentCommentIdModel
            in
            case parentProtoMessage of
                Just protoMessage ->
                    let
                        newZipper =
                            zipper
                                |> andThen (goTo (\elem -> elem.commentId == parentCommentIdModel))
                                |> andThen (insertChild (MultiwayTree.Tree (CommentModel (CommentIdModel <| Uuid.toString newUuid) commenterUserModel protoMessage.protoMessage False "" commentInitState) []))
                                |> andThen goToRoot
                    in
                    newZipper
                        |> commentUpdate isSignedIn newSeed (ClearValue parentCommentIdModel)

                Nothing ->
                    ( zipper, seed ) ! []

        CommentRouting _ ->
            ( zipper, seed ) ! []

        CommentLogin ->
            ( zipper, seed ) ! []


{-| Function to get the parent comment
-}
getParentComment : Maybe (Zipper CommentModel) -> CommentIdModel -> Maybe CommentModel
getParentComment zipper parentCommentId =
    let
        parentZipper =
            zipper
                |> andThen (goTo (\elem -> elem.commentId == parentCommentId))
    in
    Maybe.map datum parentZipper

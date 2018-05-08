module CommentTests exposing (commentTests)

import AutoExpand
import CommentModel
    exposing
        ( CommentIdModel(CommentIdModel)
        , CommentMsg(..)
        , UserCommentModel
        , UserIdModel(UserIdModel)
        , commentModelInit
        , commentZipperInit
        , config
        )
import CommentUpdate exposing (commentUpdate)
import Expect
import Fuzz
    exposing
        ( bool
        , string
        )
import MultiwayTree exposing (datum)
import MultiwayTreeZipper exposing (goTo)
import Random.Pcg
    exposing
        ( Seed
        , initialSeed
        , step
        )
import String
import Test
    exposing
        ( Test
        , describe
        , fuzz
        , test
        )
import Uuid


testSeed : Seed
testSeed =
    initialSeed 0


commentTests : Test
commentTests =
    describe "Comment Tests"
        [ test
            "GenerateCommentId Test"
          <|
            \() ->
                let
                    isErrorResult postTestMaybeZipper =
                        let
                            ( newUuid, _ ) =
                                step Uuid.uuidGenerator testSeed

                            findId =
                                postTestMaybeZipper
                                    |> Maybe.andThen (goTo (\elem -> elem.commentId == (CommentIdModel <| Uuid.toString newUuid)))

                            foundId =
                                case findId of
                                    Just _ ->
                                        True

                                    Nothing ->
                                        False
                        in
                        foundId
                in
                commentUpdate True testSeed (GenerateCommentId (UserCommentModel (UserIdModel "") "" "") (CommentIdModel "")) commentZipperInit
                    |> Tuple.first
                    |> Tuple.first
                    |> isErrorResult
                    |> Expect.true "Expected initialZipper treePortion to have a child whose id is generatedCommentId"
        , fuzz bool
            "ClickReplyButton Test"
          <|
            \testShowReplyInput ->
                let
                    newCommentModel =
                        { commentModelInit | showReplyInput = testShowReplyInput }

                    newCommentTree =
                        MultiwayTree.Tree newCommentModel []

                    newCommentZipper =
                        Just ( newCommentTree, [] )

                    isErrorResult postTestMaybeZipper =
                        let
                            zipper =
                                case postTestMaybeZipper of
                                    Just defPostTestZipper ->
                                        let
                                            postTestZipperDatum =
                                                datum (Tuple.first defPostTestZipper)
                                        in
                                        postTestZipperDatum.showReplyInput /= testShowReplyInput

                                    Nothing ->
                                        False
                        in
                        zipper
                in
                commentUpdate True testSeed (ClickReplyButton (CommentIdModel "")) newCommentZipper
                    |> Tuple.first
                    |> Tuple.first
                    |> isErrorResult
                    |> Expect.true "Expected showReplyInput property to be opposite of initial showReplyInput"
        , fuzz string
            "TextInput Test"
          <|
            \testMessage ->
                let
                    isErrorResult postTestMaybeZipper =
                        let
                            zipper =
                                case postTestMaybeZipper of
                                    Just defPostTestZipper ->
                                        let
                                            postTestZipperDatum =
                                                datum (Tuple.first defPostTestZipper)
                                        in
                                        postTestZipperDatum.protoMessage == testMessage

                                    Nothing ->
                                        False
                        in
                        zipper
                in
                commentUpdate True testSeed (TextInput (CommentIdModel "") { state = AutoExpand.initState <| config True "", textValue = testMessage }) commentZipperInit
                    |> Tuple.first
                    |> Tuple.first
                    |> isErrorResult
                    |> Expect.true "Expected protoMessage property to be identical of initial protoMessage"
        , fuzz string
            "ClearValue Test"
          <|
            \testProtoMessage ->
                let
                    testCommentModel =
                        CommentModel.CommentModel (CommentIdModel testProtoMessage) (UserCommentModel (UserIdModel "") "" "") "" False "" (AutoExpand.initState <| config False "")

                    testCommentTree =
                        MultiwayTree.Tree testCommentModel []

                    testCommentZipper =
                        Just ( testCommentTree, [] )

                    isErrorResult postTestMaybeZipper =
                        let
                            zipper =
                                case postTestMaybeZipper of
                                    Just defPostTestZipper ->
                                        let
                                            postTestZipperDatum =
                                                datum (Tuple.first defPostTestZipper)

                                            postTestZipperDataProtoMessage =
                                                postTestZipperDatum.protoMessage
                                        in
                                        String.isEmpty postTestZipperDataProtoMessage

                                    Nothing ->
                                        False
                        in
                        zipper
                in
                commentUpdate True testSeed (ClearValue (CommentIdModel testProtoMessage)) testCommentZipper
                    |> Tuple.first
                    |> Tuple.first
                    |> isErrorResult
                    |> Expect.true "Expected protoMessage property to be empty string"
        ]

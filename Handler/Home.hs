{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import


homeTitle :: MonadWidget m => m ()
homeTitle = setTitle "Akateeminen saunakerho Löyly ry"


getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost memberForm

    let submission = Nothing :: Maybe Member

    defaultLayout $ do
        homeTitle
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost memberForm

    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        homeTitle
        $(widgetFile "homepage")

memberForm :: Form Member
memberForm = renderDivs $ Member
    <$> areq textField "Koko nimi" Nothing
    <*> areq textField "Kotikunta" Nothing
    <*> areq textField "Sähköposti" Nothing

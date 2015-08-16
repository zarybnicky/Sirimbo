<?php
class Controller_Home extends Controller_Abstract
{
    public function view($request)
    {
        if (!$request->getURI()) {
            if (NABOR) {
                $this->redirect('/nabor');
            }
            $this->redirect('/home');
        }

        $articles = DBAktuality::getAktuality(AKTUALITY_CLANKY);
        $results = DBAktuality::getAktuality(AKTUALITY_KRATKE);

        $highlights = array_slice($articles, 0, 3);
        $moreArticles = array_slice($articles, 3, 9);

        $this->render(
            'files/View/Main/Home.inc',
            array(
                'highlights' => $highlights,
                'moreArticles' => $moreArticles,
                'results' => $results
            )
        );
    }
}

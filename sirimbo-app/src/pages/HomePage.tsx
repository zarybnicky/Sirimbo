import * as React from 'react';
import { HeroArticles } from './Hero';
import { ServicesList } from './ServicesList';
import { CallToAction } from '@material-ui/icons';
import { HighlightList } from './HighlightsList';
import { ArticleList } from './ArticleList';
import { VideoList } from './VideoList';

export const HomePage = () => <React.Fragment>
  <HeroArticles />
  <ServicesList />
  <CallToAction />
  <HighlightList />
  <ArticleList />
  <VideoList />
</React.Fragment>;

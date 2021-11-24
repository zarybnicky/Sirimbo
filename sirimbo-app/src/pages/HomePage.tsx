import * as React from 'react';
import { HeroArticles } from './Hero';
import { ServiceList } from './ServiceList';
import { CallToAction } from '@material-ui/icons';
import { HighlightList } from './HighlightsList';
import { ArticleList } from './ArticleList';
import { VideoList } from './VideoList';

export const HomePage = () => <React.Fragment>
  <HeroArticles />
  <ServiceList />
  <CallToAction />
  <HighlightList />
  <ArticleList />
  <VideoList />
</React.Fragment>;

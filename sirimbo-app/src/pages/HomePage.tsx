import * as React from 'react';
import { HeroArticles } from '../components/Hero';
import { ServiceList } from '../components/ServiceList';
import { CallToAction } from '../components/CallToAction';
import { HighlightList } from '../components/HighlightsList';
import { ArticleList } from '../components/ArticleList';
import { VideoList } from '../components/VideoList';

export const HomePage = () => <React.Fragment>
  <HeroArticles />
  <ServiceList />
  <CallToAction />
  <HighlightList />
  <ArticleList />
  <VideoList />
</React.Fragment>;

import * as React from 'react';
import { Hero } from '../components/Hero';
import { ServiceList } from '../components/ServiceList';
import { CallToAction } from '../components/CallToAction';
import { HighlightList } from '../components/HighlightsList';
import { ArticleList } from '../components/ArticleList';
import { VideoList } from '../components/VideoList';

export const HomePage = ({ }) => <React.Fragment>
  <Hero />
  <ServiceList />
  <CallToAction />
  <HighlightList />
  <ArticleList />
  <VideoList />
</React.Fragment>;

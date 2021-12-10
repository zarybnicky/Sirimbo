import React from 'react';
import { ComponentStory, Meta } from '@storybook/react';
import { StoryTemplate } from '../test-utils'
import { AboutPage } from './AboutPage';
import { HomePage } from './HomePage';
import { LocationsPage } from './LocationsPage';
import { NewsPage } from './NewsPage';
import { TrainersPage } from './TrainersPage';
import { GalleryPage } from './GalleryPage';

export default {
  title: 'Components/Pages',
} as Meta;

export const About: ComponentStory<typeof AboutPage> = (args) =>
  <StoryTemplate>
    <AboutPage {...args} />
  </StoryTemplate>;

export const Locations: ComponentStory<typeof LocationsPage> = (args) =>
  <StoryTemplate>
    <LocationsPage {...args} />
  </StoryTemplate>;

export const Home: ComponentStory<typeof HomePage> = (args) =>
  <StoryTemplate>
    <HomePage {...args} />
  </StoryTemplate>;

export const News: ComponentStory<typeof NewsPage> = (args) =>
  <StoryTemplate>
    <NewsPage {...args} />
  </StoryTemplate>;

export const Trainers: ComponentStory<typeof TrainersPage> = (args) =>
  <StoryTemplate>
    <TrainersPage {...args} />
  </StoryTemplate>;

export const Gallery: ComponentStory<typeof GalleryPage> = (args) =>
  <StoryTemplate>
    <GalleryPage {...args} />
  </StoryTemplate>;

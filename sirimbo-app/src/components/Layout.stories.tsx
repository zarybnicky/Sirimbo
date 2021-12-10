import React from 'react';
import { ComponentStory, Meta } from '@storybook/react';
import { StoryTemplate } from '../test-utils'
import { CallToAction } from './CallToAction';
import { DesktopHeader } from './DesktopHeader';
import { Footer } from './Footer';
import { Hero } from './Hero';
import { MobileHeader } from './MobileHeader';
import { useMockAuth } from '../use-auth';
import { mockMenu } from '../use-menu';

export default {
  title: 'Components/Layout',
} as Meta;

export const CTA: ComponentStory<typeof CallToAction> = (args) =>
  <StoryTemplate>
    <CallToAction {...args} />
  </StoryTemplate>;

export const DesktopHead: ComponentStory<typeof DesktopHeader> = () => {
  const auth = useMockAuth();
  return <StoryTemplate>
    <DesktopHeader auth={auth} menu={mockMenu} />
  </StoryTemplate>;
};

export const MobileHead: ComponentStory<typeof MobileHeader> = () => {
  const auth = useMockAuth();
  return <StoryTemplate>
    <div style={{ height: '200px' }}>
      <MobileHeader auth={auth} menu={mockMenu} />
    </div>
  </StoryTemplate>;
};

export const HeroCarousel: ComponentStory<typeof Hero> = (args) =>
  <StoryTemplate>
    <Hero {...args} />
  </StoryTemplate>;

export const Foot: ComponentStory<typeof Footer> = (args) =>
  <StoryTemplate>
    <Footer {...args} />
  </StoryTemplate>;

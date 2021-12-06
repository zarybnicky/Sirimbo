import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { MemoryRouter } from 'react-router';
import { ThemeProvider } from '@material-ui/styles';
import { ProvideAuth } from '../use-auth';
import { theme } from '../theme';

import template from './Hero.png';
import { Hero } from './Hero';

export default {
  title: 'Layout/Hero',
  component: Hero,
} as ComponentMeta<typeof Hero>;

const Template: ComponentStory<typeof Hero> = (args) =>
  <ThemeProvider theme={theme}>
    <ProvideAuth>
      <MemoryRouter>
        <Hero {...args} />
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>;

export const Normal = Template.bind({});

export const Image = () => <img src={template} />;

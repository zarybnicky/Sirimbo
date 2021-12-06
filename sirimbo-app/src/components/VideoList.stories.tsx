import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { MemoryRouter } from 'react-router';
import { ThemeProvider } from '@material-ui/styles';
import { ProvideAuth } from '../use-auth';
import { theme } from '../theme';

import template from './VideoList.png';
import { VideoList } from './VideoList';

export default {
  title: 'Layout/VideoList',
  component: VideoList,
} as ComponentMeta<typeof VideoList>;

const Template: ComponentStory<typeof VideoList> = (args) =>
  <ThemeProvider theme={theme}>
    <ProvideAuth>
      <MemoryRouter>
        <VideoList {...args} />
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>;

export const Normal = Template.bind({});

export const Image = () => <img src={template} />;

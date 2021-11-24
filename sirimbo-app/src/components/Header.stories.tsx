import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { MemoryRouter } from 'react-router';
import { ThemeProvider } from '@material-ui/styles';
import { theme } from '../theme';

import template from './Header.png';
import { Header } from './Header';

export default {
  title: 'Layout/Header',
  component: Header,
} as ComponentMeta<typeof Header>;

export const Image = () => <img src={template} />;

const Template: ComponentStory<typeof Header> = (args) =>
  <ThemeProvider theme={theme}>
    <MemoryRouter>
      <Header {...args} />
    </MemoryRouter>
  </ThemeProvider>;

export const LoggedIn = Template.bind({});
LoggedIn.args = {
  user: {},
};

export const LoggedOut = Template.bind({});
LoggedOut.args = {};

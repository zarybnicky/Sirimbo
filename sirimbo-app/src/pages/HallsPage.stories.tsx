import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { MemoryRouter } from 'react-router';
import { ThemeProvider } from '@material-ui/styles';
import { ProvideAuth } from '../use-auth';
import { theme } from '../theme';

import { HallsPage } from './HallsPage';

export default {
  title: 'Pages/HallsPage',
  component: HallsPage,
} as ComponentMeta<typeof HallsPage>;

const Template: ComponentStory<typeof HallsPage> = (args) =>
  <ThemeProvider theme={theme}>
    <ProvideAuth>
      <MemoryRouter>
        <HallsPage {...args} />
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>;

export const Normal = Template.bind({});

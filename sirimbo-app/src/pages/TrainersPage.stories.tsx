import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { MemoryRouter } from 'react-router';
import { ThemeProvider } from '@material-ui/styles';
import { ProvideAuth } from '../use-auth';
import { theme } from '../theme';

import { TrainersPage } from './TrainersPage';

export default {
  title: 'Pages/TrainersPage',
  component: TrainersPage,
} as ComponentMeta<typeof TrainersPage>;

const Template: ComponentStory<typeof TrainersPage> = (args) =>
  <ThemeProvider theme={theme}>
    <ProvideAuth>
      <MemoryRouter>
        <TrainersPage {...args} />
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>;

export const Normal = Template.bind({});

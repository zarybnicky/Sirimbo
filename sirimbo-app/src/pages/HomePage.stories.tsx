import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { MemoryRouter } from 'react-router';
import { ThemeProvider } from '@material-ui/styles';
import { ProvideAuth } from '../use-auth';
import { theme } from '../theme';

import { HomePage } from './HomePage';

export default {
  title: 'Pages/HomePage',
  component: HomePage,
} as ComponentMeta<typeof HomePage>;

const Template: ComponentStory<typeof HomePage> = (args) =>
  <ThemeProvider theme={theme}>
    <ProvideAuth>
      <MemoryRouter>
        <HomePage {...args} />
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>;

export const Normal = Template.bind({});

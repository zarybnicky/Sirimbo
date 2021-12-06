import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { MemoryRouter } from 'react-router';
import { ThemeProvider } from '@material-ui/styles';
import { ProvideAuth } from '../use-auth';
import { theme } from '../theme';

import template from './CallToAction.png';
import { CallToAction } from './CallToAction';

export default {
  title: 'Layout/CallToAction',
  component: CallToAction,
} as ComponentMeta<typeof CallToAction>;

const Template: ComponentStory<typeof CallToAction> = (args) =>
  <ThemeProvider theme={theme}>
    <ProvideAuth>
      <MemoryRouter>
        <CallToAction {...args} />
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>;

export const Normal = Template.bind({});

export const Image = () => <img src={template} />;

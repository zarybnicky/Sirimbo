import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { MemoryRouter } from 'react-router';
import { ThemeProvider } from '@material-ui/styles';
import { ProvideAuth } from '../use-auth';
import { theme } from '../theme';

import template from './ServiceList.png';
import { ServiceList } from './ServiceList';

export default {
  title: 'Layout/ServiceList',
  component: ServiceList,
} as ComponentMeta<typeof ServiceList>;

const Template: ComponentStory<typeof ServiceList> = (args) =>
  <ThemeProvider theme={theme}>
    <ProvideAuth>
      <MemoryRouter>
        <ServiceList {...args} />
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>;

export const Normal = Template.bind({});

export const Image = () => <img src={template} />;

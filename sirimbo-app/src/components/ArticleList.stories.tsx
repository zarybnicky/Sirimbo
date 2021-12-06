import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { MemoryRouter } from 'react-router';
import { ThemeProvider } from '@material-ui/styles';
import { ProvideAuth } from '../use-auth';
import { theme } from '../theme';

import template from './ArticleList.png';
import { ArticleList } from './ArticleList';

export default {
  title: 'Layout/ArticleList',
  component: ArticleList,
} as ComponentMeta<typeof ArticleList>;

const Template: ComponentStory<typeof ArticleList> = (args) =>
  <ThemeProvider theme={theme}>
    <ProvideAuth>
      <MemoryRouter>
        <ArticleList {...args} />
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>;

export const Normal = Template.bind({});

export const Image = () => <img src={template} />;

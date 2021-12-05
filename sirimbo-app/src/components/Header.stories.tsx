import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { MemoryRouter } from 'react-router';
import { ThemeProvider } from '@material-ui/styles';
import { ProvideAuth } from '../use-auth';
import { theme } from '../theme';

import template from './Header.png';
import { DesktopHeader } from './DesktopHeader';
import { MobileHeader } from './MobileHeader';
import { useMockAuth } from '../use-auth';
import { mockMenu } from '../use-menu';

export default {
  title: 'Layout/Header',
  component: DesktopHeader,
} as ComponentMeta<typeof DesktopHeader>;

const DesktopTemplate: ComponentStory<typeof DesktopHeader> = (args) => {
  const auth = useMockAuth();
  return <ThemeProvider theme={theme}>
    <ProvideAuth>
      <MemoryRouter>
        <DesktopHeader {...{ auth, menu: args.menu || mockMenu }} />
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>
};

const MobileTemplate: ComponentStory<typeof MobileHeader> = (args) => {
  const auth = useMockAuth();
  return <ThemeProvider theme={theme}>
    <ProvideAuth>
      <MemoryRouter>
        <div style={{ height: '200px' }}>
          <MobileHeader {...{ auth, menu: args.menu || mockMenu }} />
        </div>
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>
};

export const Desktop = DesktopTemplate.bind({ menu: mockMenu });
export const Mobile = MobileTemplate.bind({ menu: mockMenu })
export const Image = () => <img src={template} />;

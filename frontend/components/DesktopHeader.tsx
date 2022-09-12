import * as React from 'react';
import Link from 'components/Link';
import { AppBar, Box, Container, Toolbar, useTheme } from '@mui/material';
import { SocialButtons } from './SocialButtons';
import { AuthButton } from './AuthButton';
import { DesktopMenu } from './DesktopMenu';

import OlympLogoVertical from 'public/images/olymp-logo-vertical.svg';

export const DesktopHeader = ({ }) => {
  const theme = useTheme();

  return <AppBar position="static" color="secondary">
    <Toolbar>
      <Container maxWidth="lg" sx={{
        justifyContent: 'space-between',
        alignItems: 'center',
        display: "flex",
      }}>
        <Box sx={{
          ...theme.mixins.toolbar,
          position: 'relative',
          overflow: 'visible',
          minWidth: '104px',
        }}>
          <Box boxShadow={10} sx={{
            zIndex: 100,
            position: 'absolute',
            top: 0,
            left: 0,
            right: 0,
            width: '104px',
            height: '130px',
            backgroundColor: theme.palette.primary.main,
            color: theme.palette.primary.contrastText,
          }}>
            <Link href="/" sx={{
              display: 'block',
              padding: 0,
              margin: 0,
              height: '100%',
              width: '100%',
              position: 'relative',
            }}>
              <Box component={OlympLogoVertical} sx={{
                filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
                position: 'absolute',
                left: 0,
                bottom: 0,
                width: '104px',
                height: '104px',
                '& *': {
                  color: 'white',
                  fill: 'white !important',
                },
              }} />
            </Link>
          </Box>
        </Box>
        <DesktopMenu />
        <SocialButtons variant="medium" />
        <AuthButton />
      </Container>
    </Toolbar>
  </AppBar >;
};

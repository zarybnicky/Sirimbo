import * as React from 'react';
import Link from 'next/link';
import { Button, Container, Typography, Box, useTheme } from '@mui/material';
import type { CellPlugin } from '@react-page/editor';

import CtaImage from 'public/images/call-to-action.png';

export const CallToAction = ({ }) => {
  const theme = useTheme();
  return <Box sx={{
    backgroundColor: theme.palette.primary.main,
  }}>
    <Container maxWidth="lg" disableGutters sx={{
      backgroundImage: `url(${CtaImage})`,
      backgroundPosition: '85% 50%',
      backgroundRepeat: 'no-repeat',
      backgroundSize: 'auto 100%',
      display: 'flex',
      justifyContent: 'left',
    }}>
      <Box sx={{
        padding: '4rem',
        paddingRight: '6rem',
        background: 'linear-gradient(90deg, rgba(216,28,58,0.6) 70%, rgba(0,0,0,0) 100%)',
        [theme.breakpoints.down('sm')]: {
          padding: '2rem 1rem',
        },
      }}>
        <Typography variant="h4" component="div" sx={{
          color: 'white',
          fontWeight: 'bolder',
          fontSize: '2.5rem',
          [theme.breakpoints.down('sm')]: {
            fontSize: '1.8rem',
          },
        }}>PŘIDEJ SE K NÁM</Typography>

        <Typography variant="h5" component="div" sx={{
          fontWeight: 'bold',
          fontSize: '1.75rem',
          [theme.breakpoints.down('sm')]: {
            fontSize: '1.4rem',
          },
        }}>A OBJEV LÁSKU K TANCI</Typography>

        <Button
          LinkComponent={Link} href="/treninkove-programy"
          color="secondary" variant="contained" size="large"
          sx={{
            textTransform: 'none',
            marginTop: '1.25rem',
            padding: '.75rem 3rem',
            borderRadius: 0,
            fontSize: '140%',
          }}
        >Chci tančit</Button>
      </Box>
    </Container>
  </Box>;
};

export const CallToActionPlugin: CellPlugin<{}> = {
  Renderer: () => <CallToAction />,

  id: 'app-cta-plugin',
  title: 'Call to Action',
  description: undefined,
  version: 1,
};

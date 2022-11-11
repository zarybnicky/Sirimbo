import * as React from 'react';
import { NextLinkComposed } from 'components/Link';
import { Button, Container, Typography } from '@mui/material';
import type { CellPlugin } from '@react-page/editor';

import CtaImage from 'public/images/call-to-action.png';

export const CallToAction = ({ }) => {
  return <div className="bg-red-500">
    <Container maxWidth="lg" disableGutters sx={{
      backgroundImage: `url(${CtaImage.src})`,
      backgroundPosition: '85% 50%',
      backgroundRepeat: 'no-repeat',
      backgroundSize: 'auto 100%',
      display: 'flex',
      justifyContent: 'left',
    }}>
      <div className="px-4 py-8 md:p-16 md:pr-24" style={{
        background: 'linear-gradient(90deg, rgba(216,28,58,0.6) 70%, rgba(0,0,0,0) 100%)',
      }}>
        <Typography variant="h4" component="div" className="text-white font-black text-3xl md:text-4xl">
          PŘIDEJ SE K NÁM
        </Typography>

        <Typography variant="h5" component="div" className="font-bold text-2xl md:text-3xl">
          A OBJEV LÁSKU K TANCI
        </Typography>

        <Button
          component={NextLinkComposed} href="/treninkove-programy"
          color="secondary" variant="contained" size="large"
          sx={{
            textTransform: 'none',
            marginTop: '1.25rem',
            padding: '.75rem 3rem',
            borderRadius: 0,
            fontSize: '140%',
          }}
        >Chci tančit</Button>
      </div>
    </Container>
  </div>;
};

export const CallToActionPlugin: CellPlugin<{}> = {
  Renderer: () => <CallToAction />,

  id: 'app-cta-plugin',
  title: 'Call to Action',
  description: undefined,
  version: 1,
};

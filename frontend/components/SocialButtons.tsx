import * as React from 'react';
import { Box, BoxProps, IconButton, useTheme } from '@mui/material';
import InstagramIcon from '@mui/icons-material/Instagram';
import FacebookIcon from '@mui/icons-material/Facebook';
import YouTubeIcon from '@mui/icons-material/YouTube';

type SocialButtonsProps = { variant: 'small' | 'medium' | 'large'; } & BoxProps;

export const SocialButtons = ({ variant = 'medium', ...props }: SocialButtonsProps) => {
  const theme = useTheme();

  return <Box sx={{ textAlign: 'right' }} {...props}>
    <IconButton href="https://www.facebook.com/tkolymp" sx={{
      padding: 0,
      margin: '5px',
    }}>
      <FacebookIcon htmlColor={theme.palette.primary.main} fontSize={variant} />
    </IconButton>
    <IconButton href="https://www.instagram.com/tanecni_klub_olymp/?hl=cs" sx={{
      padding: 0,
      margin: '5px',
    }}>
      <InstagramIcon htmlColor={theme.palette.primary.light} fontSize={variant} />
    </IconButton>
    <IconButton href="https://www.youtube.com/user/TheMamcro" sx={{
      padding: 0,
      margin: '5px',
    }}>
      <YouTubeIcon htmlColor={theme.palette.grey[200]} fontSize={variant} />
    </IconButton>
  </Box>
}

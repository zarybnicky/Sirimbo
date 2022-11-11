import * as React from 'react';
import { Box, BoxProps, IconButton, useTheme } from '@mui/material';
import InstagramIcon from '@mui/icons-material/Instagram';
import FacebookIcon from '@mui/icons-material/Facebook';
import YouTubeIcon from '@mui/icons-material/YouTube';

type SocialButtonsProps = { variant: 'small' | 'medium' | 'large'; } & BoxProps;

export const SocialButtons = ({ variant = 'medium', ...props }: SocialButtonsProps) => {
  const theme = useTheme();

  return <Box sx={{ textAlign: 'right' }} {...props}>
    <IconButton className="button button-icon p-0 m-1" href="https://www.facebook.com/tkolymp">
      <FacebookIcon htmlColor={theme.palette.primary.main} fontSize={variant} />
    </IconButton>
    <IconButton className="p-0 m-1" href="https://www.instagram.com/tanecni_klub_olymp">
      <InstagramIcon htmlColor={theme.palette.primary.light} fontSize={variant} />
    </IconButton>
    <IconButton className="p-0 m-1" href="https://www.youtube.com/user/TheMamcro">
      <YouTubeIcon htmlColor={theme.palette.grey[200]} fontSize={variant} />
    </IconButton>
  </Box>
}

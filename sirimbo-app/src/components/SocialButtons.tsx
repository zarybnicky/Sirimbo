import * as React from 'react';
import { IconButton, useTheme } from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';
import InstagramIcon from '@material-ui/icons/Instagram';
import FacebookIcon from '@material-ui/icons/Facebook';
import YouTubeIcon from '@material-ui/icons/YouTube';

const useStyles = makeStyles((theme) => ({
  icon: {
    padding: 0,
    margin: '5px',
  },
  iconBox: {
    textAlign: 'right',
  },
}));

type SocialButtonsProps = { variant: 'small' | 'medium' | 'large'; } & React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>;

export const SocialButtons = ({ variant = 'medium', ...props }: SocialButtonsProps) => {
  const theme = useTheme();
  const classes = useStyles();

  return <div className={classes.iconBox} {...props}>
    <IconButton href="https://www.facebook.com/tkolymp" className={classes.icon}>
      <FacebookIcon color="primary" fontSize={variant} />
    </IconButton>
    <IconButton href="https://www.instagram.com/tanecni_klub_olymp/?hl=cs" className={classes.icon}>
      <InstagramIcon htmlColor={theme.palette.primary.light} fontSize={variant} />
    </IconButton>
    <IconButton href="https://www.youtube.com/user/TheMamcro" className={classes.icon}>
      <YouTubeIcon htmlColor="white" fontSize={variant} />
    </IconButton>
  </div>
}

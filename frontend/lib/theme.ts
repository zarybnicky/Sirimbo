import { createTheme } from '@mui/material/styles';

import './style/material-icons.css';

export const theme = createTheme({
  typography: {
    fontFamily: `"Helvetica", "Arial", sans-serif`,
    fontSize: 14,
    fontWeightLight: 300,
    fontWeightRegular: 400,
    fontWeightMedium: 500
  },
  palette: {
    primary: {
      main: "#d81c3a",
    },
    secondary: {
      main: '#222222',
    },
    background: {
      default: '#fff',
    },
  },
});

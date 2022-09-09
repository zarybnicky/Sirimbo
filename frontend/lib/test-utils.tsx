import React from 'react';
import { CssBaseline } from '@mui/material';
import { ThemeProvider } from '@mui/styles';
import { ProvideMockAuth } from 'lib/data/use-auth';
import { theme } from './theme';

export const StoryTemplate = ({ children }: { children: JSX.Element | JSX.Element[] }) => (
  <ThemeProvider theme={theme}>
    <CssBaseline />
    <ProvideMockAuth>
      {children}
    </ProvideMockAuth>
  </ThemeProvider>
);

export function getPlaceholder(
  width = 300,
  height = 150,
  text = `${width}×${height}`,
  fontFamily = 'sans-serif',
  fontWeight = 'bold',
  fontSize = Math.floor(Math.min(width, height) * 0.2),
  dy = fontSize * 0.35,
  bgColor = '#ddd',
  textColor = 'rgba(0,0,0,0.5)',
  dataUri = true,
  charset = 'UTF-8',
) {
  const str = `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">
    <rect fill="${bgColor}" width="${width}" height="${height}"/>
    <text fill="${textColor}" font-family="${fontFamily}" font-size="${fontSize}" dy="${dy}" font-weight="${fontWeight}" x="50%" y="50%" text-anchor="middle">${text}</text>
  </svg>`;
  const cleaned = str.replace(/[\t\n\r]/gim, '').replace(/\s\s+/g, ' ').replace(/'/gim, '\\i');
  if (dataUri) {
    const encoded = encodeURIComponent(cleaned).replace(/\(/g, '%28').replace(/\)/g, '%29');
    return `data:image/svg+xml;charset=${charset},${encoded}`;
  }
  return cleaned;
}

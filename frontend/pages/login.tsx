import * as React from 'react';
import { Container } from '@mui/material';
import { LoginForm } from 'components/LoginForm';

export default function LoginPage() {
  return (
    <Container maxWidth="xs" style={{ margin: '4rem auto 6rem' }}>
      <LoginForm />
    </Container>
  );
};

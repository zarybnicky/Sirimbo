import { LoginForm } from '@app/ui/LoginForm';
import type { NextPageWithLayout } from 'pages/_app';
import * as React from 'react';

const Page: NextPageWithLayout = () => <LoginForm successHref="/dashboard" />

Page.staticTitle = "Přihlášení";
Page.requireLoggedOut = true;

export default Page;

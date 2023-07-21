import { CallToAction } from '@app/branding-olymp/CallToAction';
import Contact from '@app/branding-olymp/Contact';
import { Heading } from '@app/ui/Heading';
import { useAuth } from '@app/ui/use-auth';
import type { NextPageWithLayout } from 'pages/_app';
import * as React from 'react';

const Page: NextPageWithLayout = () => {
  const { user } = useAuth();

  return (
    <>
      <Heading>Kontakt</Heading>
      <Contact />
      {!user && <CallToAction url="/kontakt" />}
    </>
  );
}

Page.staticTitle = "Kontakt";
Page.hideTopMenuIfLoggedIn = true;

export default Page;

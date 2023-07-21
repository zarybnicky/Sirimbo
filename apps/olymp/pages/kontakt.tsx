import Contact from 'components/Contact';
import { Heading } from '@app/ui/Heading';
import * as React from 'react';
import { NextSeo } from 'next-seo';
import { Layout } from 'components/layout/Layout';

const Page = () => {
  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title="Kontakt" />
      <Heading>Kontakt</Heading>
      <Contact />
    </Layout>
  );
}

export default Page;

import * as React from 'react';
import { CallToAction } from '@app/branding-olymp/CallToAction';
import Trainers from '@app/branding-olymp/Trainers';
import type { NextPageWithLayout } from 'pages/_app';
import { Heading } from '@app/ui/Heading';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <Heading>Naši trenéři</Heading>
      <Trainers />
      <CallToAction />
    </>
  );
}

Page.staticTitle = "Trenéři";
Page.showTopMenu = true;

export default Page;

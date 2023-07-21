import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import Trainers from 'components/Trainers';
import type { NextPageWithLayout } from 'pages/_app';
import { Heading } from '@app/ui/Heading';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <Heading>Naši trenéři</Heading>
      <Trainers />
      <CallToAction url="/treneri" />
    </>
  );
}

Page.staticTitle = "Trenéři";
Page.showTopMenu = true;

export default Page;

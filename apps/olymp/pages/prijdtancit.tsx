import * as React from 'react';
import type { NextPageWithLayout } from 'pages/_app';
import PrijdTancit from 'components/PrijdTancit';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <PrijdTancit />
    </>
  );
};

Page.showTopMenu = true;
Page.staticTitle = 'Přijď tančit!';

export default Page;

import React from 'react';
import { Calendar } from '@app/ui/calendar/Calendar';
import { Layout } from 'components/layout/Layout';

const Page = () => (
  <Layout requireMember>
    <Calendar />
  </Layout>
);


export default Page;

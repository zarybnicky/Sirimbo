import { ReservationForm } from '@app/ui/ReservationForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import { ReservationList } from '@app/ui/entity-lists';
import { Reservation } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peNabidka, PermissionLevel.P_OWNED]}>
    <NextSeo title="Nabídky" />
    <WithSidebar sidebar={<ReservationList />}>
      <ReservationForm entity={Reservation} id={fromSlugArray(useRouter().query.id)}/>
    </WithSidebar>
  </Layout>
);

export default Page;

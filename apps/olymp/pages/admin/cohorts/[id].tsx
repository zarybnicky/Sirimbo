import { CohortForm } from '@app/ui/CohortForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { CohortList } from '@app/ui/entity-lists';
import { Cohort } from '@app/ui/entities';
import { WithEntity } from '@app/ui/generic/WithEntity';

const Page: NextPageWithLayout = () => (
  <WithEntity
    perms={[PermissionKey.peAktuality, PermissionLevel.P_OWNED]}
    fetcher={CohortForm.fetcher}
    id={fromSlugArray(useRouter().query.id)}
  >
    <CohortForm entity={Cohort} />
  </WithEntity>
);

Page.list = <CohortList />;
Page.isDetail = true;
Page.staticTitle = "Skupiny";

export default Page;

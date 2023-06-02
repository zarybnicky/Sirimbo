import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { DeleteButton } from 'components/DeleteButton';
import { fromSlugArray } from 'lib/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { CoupleDocument, DeleteCoupleDocument } from 'lib/graphql/Couple';
import { LessonButton } from 'components/LessonButton';
import { CoupleList } from 'lib/entity-lists';
import { formatCoupleName } from 'lib/format-name';
import { useQuery } from 'urql';
import { TitleBar } from 'components/layout/TitleBar';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const [{ data }] = useQuery({query: CoupleDocument, variables: { id },pause: !id });
  const item = data?.pary;
  if (!item) {
    return null;
  }
  return (
    <div className="container py-4">
      <TitleBar backHref="/admin/pary" title={formatCoupleName(item)}>
        <DeleteButton
          doc={DeleteCoupleDocument}
          id={id}
          onDelete={() => router.push('/admin/nastenka')}
          title="smazat pár"
        />
      </TitleBar>

      Minulé lekce
      {item.rozpisItemsByRiPartner?.nodes.map((item) => (
        <LessonButton key={item.id} schedule={item.rozpiByRiIdRodic!} lesson={item} showTrainer showDate />
      ))}
    </div>
  );
};

Page.list = <CoupleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.pePary, PermissionLevel.P_OWNED];
Page.staticTitle = "Páry";

export default Page;

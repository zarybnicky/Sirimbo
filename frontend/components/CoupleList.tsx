import { useCoupleListQuery, useFixUnpairedCouplesMutation } from "lib/graphql/Couple";
import { Plus } from 'react-feather';
import { useRouter } from "next/router";
import { List } from "components/layout/List";
import React from "react";
import { toast } from "react-toastify";

export function CoupleList() {
  const router = useRouter();
  const { data, refetch } = useCoupleListQuery();
  const { mutateAsync: doFix } = useFixUnpairedCouplesMutation({
    onSuccess: () => refetch(),
  });

  const { id } = router.query;
  const active = id ? id as string : null;

  const fix = React.useCallback(async () => {
    const data = await doFix({});
    toast.info(`Opraveno ${data.fixUnpairedCouples?.paries?.length || 0} záznamů`);
  }, [doFix]);

  return <List>
    <List.TitleBar title="Páry">
      <List.TitleButton active={router.asPath.endsWith('add')} icon={Plus} href="/admin/pary/add">
        Nový pár
      </List.TitleButton>

      <div className="mt-2 w-full flex gap-2 justify-end">
        <List.TitleButton onClick={fix}>Opravit nespárované páry</List.TitleButton>
      </div>
    </List.TitleBar>

    <List.Scroll>
      {data?.activeCouples?.nodes?.map((item) => (
        <List.Item
          key={item.id}
          active={active === item.id}
          href={`/admin/pary/${item.id}`}
          title={`${item.userByPIdPartner?.uPrijmeni}-${item.userByPIdPartnerka?.uPrijmeni}`}
        />
      ))}
    </List.Scroll>
  </List>;
}

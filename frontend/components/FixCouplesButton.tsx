import { toast } from 'react-toastify';
import { useGqlMutation } from 'lib/query';
import { FixUnpairedCouplesDocument } from 'lib/graphql/Couple';
import { useQueryClient } from '@tanstack/react-query';
import React from 'react';
import { List } from './layout/List';

export const FixCouplesButton = () => {
  const queryClient = useQueryClient();
  const { mutateAsync: doFix } = useGqlMutation(FixUnpairedCouplesDocument, {
    onSuccess() {
      queryClient.invalidateQueries(['CoupleList']);
    },
  });

  const fix = React.useCallback(async () => {
    const data = await doFix({});
    toast.info(`Opraveno ${data.fixUnpairedCouples?.paries?.length || 0} záznamů`);
  }, [doFix]);

  return <List.TitleButton onClick={fix}>Opravit nespárované páry</List.TitleButton>;
};

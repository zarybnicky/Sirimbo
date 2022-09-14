import { useTypedQuery } from "lib/query";

export const useCohorts = () => {
  const { data } = useTypedQuery(['cohorts'], {
    skupinies: [
      { condition: { sVisible: true } },
      {
        nodes: {
          __typename: true,
          sId: true,
          sName: true,
          sLocation: true,
          sDescription: true,
          sVisible: true,
          sColorRgb: true,
        }
      },
    ]
  });

  return data?.skupinies?.nodes || [];
};

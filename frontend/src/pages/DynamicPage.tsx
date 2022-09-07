import * as React from 'react';
import { gql, useQuery } from '@apollo/client';
import { useLocation } from 'react-router-dom';
import { ReactPage } from '../components/ReactPage';
import { Container, Typography } from '@material-ui/core';

const GET_PAGE = gql(`
query GetPage($url: String!) {
  pageByUrl(url: $url) {
    content
  }
}`);

export const DynamicPage = () => {
  const location = useLocation();
  const { data } = useQuery(GET_PAGE, { variables: { url: location.pathname } });
  return <ReactPage readOnly value={data?.pageByUrl?.content} />;
}

export const OldDynamicPage = () => {
  const location = useLocation();
  const [json, setJson] = React.useState<any>({});;
  React.useEffect(() => {
    (async () => {
      console.log('fetching', `${location.pathname}?no-layout=1`);
      const res = await fetch(`${location.pathname}?no-layout=1`);
      const json = await res.json();
      setJson(json);
    })();
  }, []);
  return <Container maxWidth="lg" style={{ marginTop: 80 }}>
    <Typography variant="h3" component="h2">{json.title}</Typography>

    <div dangerouslySetInnerHTML={{ __html: json.content }} />
  </Container>;
}

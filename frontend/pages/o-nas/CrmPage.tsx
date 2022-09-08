import * as React from 'react';
import format from 'date-fns/format';
import { Container, Typography } from '@material-ui/core';
import { useTypedQuery } from '../zeus/apollo';
import { Selector } from '../zeus';

import Table from '@material-ui/core/Table';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableContainer from '@material-ui/core/TableContainer';
import TableHead from '@material-ui/core/TableHead';
import TableRow from '@material-ui/core/TableRow';
import Paper from '@material-ui/core/Paper';

const ProspectQuery = Selector('Query')({
  activeProspects: [
    {},
    {
      totalCount: true,
      nodes: {
        id: true,
        data: {
          name: true,
          surname: true,
          email: true,
          phone: true,
          yearofbirth: true,
        },
        cohort: true,
        updatedAt: true,
      },
    },
  ],
});

export const CrmPage = ({ }) => {
  const { data } = useTypedQuery(ProspectQuery);
  const nodes = data?.activeProspects?.nodes || [];

  return <Container maxWidth="lg" style={{ padding: '2rem 0' }}>
    <Typography align="right" variant="h4" component="h2">Zájemci</Typography>

    <TableContainer component={Paper}>
      <Table style={{ minWidth: 850 }}>
        <TableHead>
          <TableRow>
            <TableCell>Jméno</TableCell>
            <TableCell>E-mail</TableCell>
            <TableCell>Telefon</TableCell>
            <TableCell>Rok narození</TableCell>
            <TableCell>Zdroj</TableCell>
            <TableCell>Poslední aktivita</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {nodes.map((row) => (
            <TableRow key={row.id}>
              <TableCell><b>{row.data?.name} {row.data?.surname}</b></TableCell>
              <TableCell>{row.data?.email}</TableCell>
              <TableCell>{row.data?.phone}</TableCell>
              <TableCell>{row.data?.yearofbirth}</TableCell>
              <TableCell>{row.cohort}</TableCell>
              <TableCell>{format(new Date(row.updatedAt), 'd. M. y')}</TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </TableContainer>
  </Container>;
}

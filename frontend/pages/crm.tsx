import * as React from 'react';
import format from 'date-fns/format';
import { Container, Typography } from '@mui/material';
import Table from '@mui/material/Table';
import TableBody from '@mui/material/TableBody';
import TableCell from '@mui/material/TableCell';
import TableContainer from '@mui/material/TableContainer';
import TableHead from '@mui/material/TableHead';
import TableRow from '@mui/material/TableRow';
import Paper from '@mui/material/Paper';
import { useActiveProspectsQuery } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';

export default function CrmPage() {
  useRequireUserLoggedIn()
  const { data } = useActiveProspectsQuery();
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
              <TableCell>{row.updatedAt && format(new Date(row.updatedAt), 'd. M. y')}</TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </TableContainer>
  </Container>;
}

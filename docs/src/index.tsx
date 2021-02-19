import React from 'react';
import ReactDOM from 'react-dom';
import { Voyager } from 'graphql-voyager';
import { buildSchema, introspectionFromSchema } from 'graphql';
  
ReactDOM.render(<Voyager
  introspection={{ data: introspectionFromSchema(buildSchema(require('./schema.graphql')))}}
  workerURI={process.env.PUBLIC_URL + '/voyager.worker.js'}
/>, document.getElementById('root'));

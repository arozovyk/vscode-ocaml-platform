import React from "react";
import ReactDOM from "react-dom";
import '../css/style.css';
import InteractorFactory from './Interaction/InteractorFactory';
import "regenerator-runtime/runtime";
import ASTOutput from './components/ASTOutput';
import astExample from './containers/test.json'
import getTreeAdapter from './parserMiddleware'
import newParser from './parsers/refmt-ml'
const Interactor = InteractorFactory.create();

class Index extends React.Component {

  constructor(props) {
    super(props);
    this.state = { treeAdapter: getTreeAdapter(newParser) }
    this.state = { 
      astResult: {
        ast: null,
        error: null,
        time: 120,
        treeAdapter: this.state.treeAdapter
      }
    }
    window.addEventListener('message', event => {
      var parseRes =event.data.value
       
      //console.log(JSON.stringify(parseRes));
      //console.log(JSON.stringify(astExample));


      this.setState({
        astResult: {
          ast: parseRes,
          error: null,
          time: 120,
          treeAdapter: getTreeAdapter(newParser)
        }
      });
    });

  }




  updateFilesToDisplay() {
    Interactor.getDirectoryInfo(directoryInfo => {
      this.setState({ directoryInfo: directoryInfo });
    })
  }

  render() {



    return <>
      <div className="container">
        <ASTOutput parseResult={this.state.astResult} position={0} />
        {this.message}
      </div>
    </>
  }
}

ReactDOM.render(<Index />, document.getElementById("index"));